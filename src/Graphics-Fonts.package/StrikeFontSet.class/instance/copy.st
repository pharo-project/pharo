copying
copy

	| s a |
	s := self class new.
	s name: self name.
	s emphasis: self emphasis.
	s reset.
	a := Array new: fontArray size.
	1 to: a size do: [:i |
		a at: i put: (fontArray at: i) copy.
	].
	s fontArray: a.
	^ s.
