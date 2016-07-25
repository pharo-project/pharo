testing
testBlocking

	| pipe |
	pipe := self blockingPipe.
	(1 to: 10) do: [:i | pipe nextPutAll: 'this is line ', i printString; cr].
	pipe flush. "protect against buggy clib that may never flush output"
	self should: ['this is line 1*' match: (pipe next: 15)].
	self should: ['this is line 2*' match: (pipe next: 15)].
	self should: ['this is line 3*' match: (pipe next: 15)].
	self should: ['this is line 4*' match: (pipe next: 15)].
	self should: ['this is line 5*' match: (pipe next: 15)].
	self should: ['this is line 6*' match: (pipe next: 15)].
	self should: ['this is line 7*' match: (pipe next: 15)].
	self should: ['this is line 8*' match: (pipe next: 15)].
	self should: ['this is line 9*' match: (pipe next: 15)].
	pipe writer close.
	self shouldnt: pipe atEnd.
	self should: ['this is line 10*' match: (pipe next: 16)].
	self should: pipe atEnd.
	pipe close
