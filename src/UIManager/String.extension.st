Extension { #name : #String }

{ #category : #'*UIManager-Support' }
String >> displayProgressAt: aPoint from: minVal to: maxVal during: workBlock [ 
	"Deprecated! - Display this string as a caption over a progress bar while workBlock is evaluated.

EXAMPLE (Select next 6 lines and Do It)
'Now here''s some Real Progress'
	displayProgressAt: Sensor cursorPoint
	from: 0 to: 10
	during: [:bar |
	1 to: 10 do: [:x | bar value: x.
			(Delay forMilliseconds: 500) wait]].

HOW IT WORKS (Try this in any other language :-)
Since your code (the last 2 lines in the above example) is in a block,
this method gets control to display its heading before, and clean up 
the screen after, its execution.
The key, though, is that the block is supplied with an argument,
named 'bar' in the example, which will update the bar image every 
it is sent the message value: x, where x is in the from:to: range.
"
	"deprecated use displayProgressFrom: minVal to: maxVal during: workBlock "

	^UIManager default 
		displayProgress: self
		from: minVal 
		to: maxVal 
		during: workBlock
]

{ #category : #'*UIManager-Support' }
String >> displayProgressFrom: minVal to: maxVal during: workBlock [ 
	"Display this string as a caption over a progress bar while workBlock is evaluated.

EXAMPLE (Select next 6 lines and Do It)
'Now here''s some Real Progress'
	displayProgressFrom: 0 to: 10
	during: [:bar |
	1 to: 10 do: [:x | bar value: x.
			(Delay forMilliseconds: 500) wait]].
"
	^ UIManager default 
		displayProgress: self 
		from: minVal 
		to: maxVal 
		during: workBlock
]
