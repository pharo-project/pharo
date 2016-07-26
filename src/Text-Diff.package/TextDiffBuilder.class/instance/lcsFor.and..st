private
lcsFor: xFilteredLines and: yFilteredLines
	"I find one of the longest common subsequences of my the arguments. I assume that none of my arguments are empty. I return nil or an Array which represents a list. The first two elements are the matching line numbers, the last is the next node in the list or nil if there are no more elements. The list containts the longest common subsequence. I'm a modified version of the greedy lcs algorithm from the 6th page of 'An O(ND) Difference Algorithm and Its Variations (1986)' by Eugene W. Myers"

	| n m v lcss max |
	n := xFilteredLines size.
	m := yFilteredLines size.
	max := m + n.
	v := Array new: 2 * max + 1.
	v at: max + 2 put: 0.
	lcss := Array new: 2 * max + 1.
	0 to: max do: [ :d |
		d negated to: d by: 2 do: [ :k |
			| index lcs x y |
			index := max + k.
			(k + d = 0 or: [ k ~= d and: [ (v at: index) < (v at: index + 2) ] ])
				ifTrue: [ x := v at: (index := index + 2) ]
				ifFalse: [ x := (v at: index) + 1 ].
			y := x - k.
			lcs := lcss at: index.
			[ x < n and: [ y < m and: [ (xFilteredLines at: x + 1) = (yFilteredLines at: y + 1) ] ] ]
				whileTrue: [
					lcs := { x := x + 1. y := y + 1. lcs } ].
			(x >= n and: [ y >= m ]) ifTrue: [ ^lcs ].
			v at: max + k + 1 put: x.
			lcss at: max + k + 1 put: lcs ] ].
	self error