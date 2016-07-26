private
setTarget: aMorph
	(target := aMorph) ifNotNil: [offsetFromTarget := self position - target position]