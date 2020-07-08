package proust

object sequencing {

import disjoining._
import option._

trait L[A] {

  def head: A

  def headOption: O[A]
}

object L {

  def nil[A]: L[A] =
    ???

  def cons[A]: L[A] =
    ???
}}