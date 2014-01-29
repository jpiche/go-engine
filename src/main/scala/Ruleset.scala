package net.semeai.go

sealed trait Ruleset
case object AGA extends Ruleset
case object Japanese extends Ruleset
case object Chinese extends Ruleset

