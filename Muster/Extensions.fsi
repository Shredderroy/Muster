namespace Muster.Extensions


module ListExtensions =


    val mapThread : (list<'A> -> 'B) -> (list<list<'A>>) -> list<'B>


    val transpose : list<list<'A>> -> list<list<'A>>


    val pickFromList : list<'A> -> list<int> -> list<'A>


module StringExtensions =


    val getNonEmptyTokens : string -> list<string>


    val getNGrams : string -> list<string>


    val getShingles : string -> list<string>

