package net.semeai.go


object Ruleset extends RulesetRules

sealed trait RulesetRules {
  final val AGA       = 0
  final val Japanese  = 1
  final val Chinese   = 2
}
