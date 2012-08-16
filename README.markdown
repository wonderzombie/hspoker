## hspoker

### todo

*   <strike>fix straights wrt aces high OR low. homework uses a lookup, mapping cards to rank via an array (read: string). `A` appears twice, though— what about the case where `A` needs to be high? e.g. `T, J, Q, K, A`.</strike> -- done.

*   organize Poker.hs. functions are all jumbled.
*   possibly break out a separate PokerUtil.hs.
*   consider refactoring getSuit into a `lookup`.

### maybe todo

*   tests probably need cleaning up refactoring in general, but my inexperience is working against me here. not sure what best practices are.
*   maybe hand-related tests should assert on the rank returned?
*   consider ending experiment with slavish use of `case`.

### notes

*   n/a for now