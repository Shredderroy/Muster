namespace Muster.Extensions


module ListExtensions =


    val mapThread : (list<'A> -> 'B) -> (list<list<'A>>) -> list<'B>


    val transpose : list<list<'A>> -> list<list<'A>>


    val pickFromList : list<'A> -> list<int> -> list<'A>

