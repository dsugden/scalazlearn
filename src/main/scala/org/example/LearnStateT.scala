package org.example


import scalaz._
import Scalaz._
import scalaz.effect.IO

object LearnStateT extends App with StateTFunctions {

  case class DB(v: Int)

  val initial = DB(1)


  type EitherString[+T]  = \/[String, T]

  type OptionStateT[T] = StateT[Option, DB, T]

  type EitherStateT[T] = StateT[EitherString, DB, T]

  type IOState[T] = StateT[IO,DB,T]

  val initialOptionState: OptionStateT[Unit] = constantStateT[Option, DB, Unit]({})(initial)

  val initialEitherState: EitherStateT[Unit] = constantStateT[EitherString, DB, Unit]({})(initial)

  val initialIOState: IOState[Unit] = constantStateT[IO, DB, Unit]({})(initial)


  def addWithOption(newV: Int): StateT[Option, DB, Int] =
    if (newV < 0)
      StateT[Option, DB, Int](s => None)
    else
      StateT[Option, DB, Int](s => Some {
        val newvalue = s.v + newV
        (s.copy(v = newvalue), newvalue)
      })

  def addWithEither(newV: Int): StateT[EitherString, DB, Int] =
    if (newV < 0)
      StateT[EitherString, DB, Int](s => -\/("Number must be > 0"))
    else
      StateT[EitherString, DB, Int](s => \/- {
        val newvalue = s.v + newV
        (s.copy(v = newvalue), newvalue)
      })


  def intIO(i:Int,db:DB):IO[(DB,Int)] = IO{
    if( i < 0){
      println(s" $i is less than 0")
      (db,i)
    }
    else{
      println("Doing some DB update")
      val newvalue = db.v + i
      (db.copy(v = newvalue), newvalue)
    }
  }

  def addWithIO(newV: Int): StateT[IO, DB, Int] =
      StateT[IO,DB,Int](s => intIO(newV,s))




  val computeSomeStuffWithOption = initialOptionState.flatMap(x => addWithOption(2)).flatMap(x => addWithOption(3))
  println("OPTION 1 " +computeSomeStuffWithOption.run(initial))

  val computeSomeStuffWithOption2 = initialOptionState.flatMap(x => addWithOption(-1))
  println("OPTION 2" +computeSomeStuffWithOption2.run(initial).getOrElse("ERROR, None"))



  val computeSomeStuffWithEither = initialEitherState.flatMap(x => addWithEither(2)).flatMap(x => addWithEither(3))
  println("EITHER 1 " +computeSomeStuffWithEither.run(initial))

  val computeSomeStuffWithEither2 = initialEitherState.flatMap(x => addWithEither(-1))

  val runit = computeSomeStuffWithEither2.run(initial).fold(left => {
    // got an error, try with another number
    initialEitherState.flatMap(x => addWithEither(5)).run(initial)
  },right => right )

  println("EITHER 2" +runit)


  val computeSomeStuffWithIO = initialIOState.flatMap(x => addWithIO(2)).flatMap(x => addWithIO(3)).flatMap(x => addWithIO(-1))
  println("IO 1 " +computeSomeStuffWithIO.run(initial).unsafePerformIO())





}
