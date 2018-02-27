open System
open System.Net.Http
open System.Text
open Newtonsoft.Json

[<AutoOpen>]
module Fx =
    let inline tryInvoke f a = try f a |> Some with _ -> None
    
    let tap a b = a b; b

    let result (t : System.Threading.Tasks.Task<_>) = t.Result

module String =
    let empty arg = String.IsNullOrWhiteSpace(arg)

    let toUtf8 (arg: string) = System.Text.Encoding.UTF8.GetBytes(arg)

    let toBase64 bytes = Convert.ToBase64String(bytes)

module Rest = 
    let private addHeader (headers: Headers.HttpHeaders) (name, value: string) =
        headers.Add (name, value)
    
    let private addBody (req: HttpRequestMessage) headers body =
        req.Content <- new StringContent (body)
        let contentTypeHeader = 
            headers |> List.tryFind (fun (n, _) -> n = "Content-Type")
        contentTypeHeader 
        |> Option.iter (fun (_, v) -> req.Content.Headers.ContentType.MediaType <- v)

    let composeMessage meth (url : Uri) headers body =
        let req = new HttpRequestMessage (meth, url)
        Option.iter (addBody req headers) body

        headers
        |> List.partition (fun (n, _) -> n = "Content-Type")
        |> snd
        |> List.iter (addHeader req.Headers)
        req

    let post url headers body =
        use client = new HttpClient ()
        composeMessage Net.Http.HttpMethod.Post (Uri url) headers (Some body)
        |> tap (printfn "%A")
        |> client.SendAsync
        |> result

module TogglRest = 

    let private getTogglApiUrl = sprintf "https://www.toggl.com/api/v8/%s"

    let private toAuthHeader token = sprintf "Basic %s" (String.toBase64(String.toUtf8(sprintf "%s:api_token" token)))
    
    let private postHeaders token = 
        [("Content-Type", "application/json"); ("Authorization", (toAuthHeader token))] 

    let readContent (content: HttpContent) = 
        async {
            let! body = content.ReadAsStringAsync() |> Async.AwaitTask
            return body
        }

    let createTimeEntry token entry =
        let url = getTogglApiUrl "time_entries"
        printfn "%s" url
        Rest.post url (postHeaders token) entry 
        |> tap (fun r -> (readContent r.Content) |> Async.RunSynchronously |> printfn "%A")
        

[<CLIMutable>]
type TimeEntry = { 
    [<JsonProperty("description") >]
    Description: string
    [<JsonProperty("duration")>]
    Duration: int
    [<JsonProperty("pid")>]
    Pid: string
    [<JsonProperty("start")>]
    Start: DateTime
    [<JsonProperty("created_with")>]
    CreatedBy: string
}

type TimeEntryWrapper = {
    [<JsonProperty("time_entry")>]
    TimeEntry: TimeEntry
}


   
let getDateSeqFor ((start: DateTime), (endd: DateTime)) =     
    Seq.unfold (fun date -> if date <= endd then Some(date, date.AddDays(1.0)) else None) start

let excludeWeekends =
    Seq.filter (fun (day: DateTime) -> (day.DayOfWeek <> DayOfWeek.Saturday) && (day.DayOfWeek <> DayOfWeek.Sunday)) 
    
let excludeAbsences absences days = 
    let L = set days
    let R = set absences
    L - R |> Set.toSeq


let toTimeEntry description pid ((startTime: DateTime), (endTime: DateTime)) =
    let duration = endTime.Subtract(startTime)
    { 
        TimeEntry = 
        {
            Description = description;
            Duration = int duration.TotalSeconds;
            Pid = pid;
            Start = startTime.ToUniversalTime();
            CreatedBy = "SharpToggl"
        }
    }

let mapToTimeEntries description pid (days: seq<DateTime>) = 
    Seq.map (fun (d: DateTime) -> toTimeEntry description pid (d.AddHours(9.0), d.AddHours(18.0))) days

let mapToJson (timeEntries: seq<TimeEntryWrapper>) = 
    Seq.map JsonConvert.SerializeObject timeEntries

let postEntriesToToggl token (jsonTimeEntries: seq<string>) = 
    Seq.iter (fun t -> TogglRest.createTimeEntry token t |> ignore) jsonTimeEntries

[<EntryPoint>]
let main argv = 
    let period = (DateTime(2018, 2, 01), DateTime(2018, 2, 05))
    let absences = [DateTime(2018, 2, 01); DateTime(2018, 2, 09)]

    let token = "f60f768a28056afce058c93a1b5e77ea"
    let devPid = "18898858"
    
    getDateSeqFor period
    |> excludeWeekends
    |> excludeAbsences absences
    |> tap (printfn "%A")
    |> mapToTimeEntries "GetBusy dev" devPid
    |> mapToJson
    |> postEntriesToToggl token

    System.Console.ReadLine() |> ignore
    0 // return an integer exit code

