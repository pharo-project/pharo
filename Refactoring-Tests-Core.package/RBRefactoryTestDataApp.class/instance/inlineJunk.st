test
inlineJunk
	| asdf |
	asdf := self inlineFoo: 
					[:each | 
					| temp |
					temp := each.
					temp , temp].
	^asdf foo: 
			[:bar | 
			| baz |
			baz := bar.
			baz * baz]