package net.semeai.go

import java.io.FileReader

import scala.util.parsing.combinator._

object app extends App {
  val g = Game(5)
  println(g.board.draw)
  val g2 = g + Point(0, 0) + Point(1,1) + Point(3,3) + Point(0, 4) + Point(0,1) + Point(0, 2)
  println(g2.board.draw)

  val g3 = SGF.parse("""
(;GM[1]FF[4]SZ[19]KM[5.5]HA[0]
  ;B[pd];W[dp];B[pp];W[dd];B[cn];W[co];B[dn];W[fc];B[ep];W[eq];B[dj];W[qn]
  ;B[cf];W[df];B[dg];W[de];B[nq];W[qk];B[qo];W[ro];B[rp];W[rn];B[fp];W[fq]
  ;B[dq];W[cq];B[dr];W[cr];B[do];W[cp];B[pn];W[pm];B[lc];W[qf];B[pf];W[pg]
  ;B[of];W[og];B[qe];W[rf];B[re];W[qg];B[on];W[om];B[iq];W[hr];B[hq];W[gq]
  ;B[ir];W[gr];B[gp];W[qq];B[qp];W[nn];B[rq];W[oo];B[po];W[np];B[mq];W[mp]
  ;B[ic];W[eg];B[dh];W[eh];B[io];W[ko];B[op];W[no];B[jm];W[km];B[ei];W[fi]
  ;B[fj];W[fh];B[kl];W[jl])
""")
  println(g3.get.root.tree.drawTree)

  val g4 = SGF.parse("""
(;GM[1]FF[4]CA[UTF-8]AP[CGoban:3]ST[2]
RU[Japanese]SZ[19]KM[0.00]
PW[White]PB[Black]
;B[dd]
;W[pp]C[asdf]
;B[pd]C[\]]
;W[ek]C[[]
;B[in]C[sfdgfhgh!@#$%^&*()asshgfb

nmb}+_0987689809-0=\][]
;W[nf]C[iugfx]
;B[]
;W[]
;B[]
;W[])
""")
  println(g4.get.root.tree.drawTree)

  val g5 = SGF.parse("""
(;GM[1]FF[4]CA[UTF-8]AP[CGoban:3]ST[2]
RU[Japanese]SZ[9]KM[0.00]
PW[White]PB[Black]
;B[dh]
(;W[gg]
(;B[gc])
(;B[cc]))
(;W[cc]
;B[gc])
(;W[cg]
;B[dg]
;W[cf]))
""")
  println(g5 map { g =>
    g.root.tree.drawTree
  } getOrElse g5)

}

