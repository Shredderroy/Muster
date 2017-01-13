namespace MusterLib


open System
open System.IO
open MBrace.FsPickler.Json


module Pickle =


    let serialiseData<'A> (dat : 'A) (fileLoc : string) : unit =
        let jsonSerialiser = FsPickler.CreateJsonSerializer(indent = true)
        let pckl = jsonSerialiser.Pickle dat
        File.WriteAllBytes(fileLoc, pckl)


    let deserialiseData<'A> (fileLoc : string) : 'A =
        let byteArr = File.ReadAllBytes fileLoc
        let jsonSerialiser = FsPickler.CreateJsonSerializer(indent = true)
        jsonSerialiser.UnPickle<'A> byteArr

