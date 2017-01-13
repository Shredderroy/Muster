namespace MusterLib


open System
open System.IO
open MBrace.FsPickler
open MBrace.FsPickler.Json


module Pickle =


    let serialiseDataJson<'A> (dat : 'A) (fileLoc : string) : unit =
        let jsonSerialiser = FsPickler.CreateJsonSerializer(indent = true)
        let pckl = jsonSerialiser.Pickle dat
        File.WriteAllBytes(fileLoc, pckl)


    let deserialiseDataJson<'A> (fileLoc : string) : 'A =
        let byteArr = File.ReadAllBytes fileLoc
        let jsonSerialiser = FsPickler.CreateJsonSerializer(indent = true)
        jsonSerialiser.UnPickle<'A> byteArr


    let serialiseData<'A> (dat : 'A) (fileLoc : string) : unit =
        let binSerialiser = FsPickler.CreateBinarySerializer()
        let pckl = binSerialiser.Pickle dat
        File.WriteAllBytes(fileLoc, pckl)


    let deserialiseData<'A> (fileLoc : string) : 'A =
        let byteArr = File.ReadAllBytes fileLoc
        let binSerialiser = FsPickler.CreateBinarySerializer()
        binSerialiser.UnPickle<'A> byteArr

