Extension { #name : 'String' }

{ #category : '*Jobs' }
String >> displayProgressFrom: minVal to: maxVal during: workBlock [
	"Display this string as a caption over a progress bar while workBlock is evaluated.

EXAMPLE (Select next 6 lines and Do It)
'Now here''s some real progress'
	displayProgressFrom: 0 to: 10
	during: [:bar |
	1 to: 10 do: [:x | bar value: x.
			(Delay forMilliseconds: 500) wait]].
"
	^ workBlock asJob
			title: self;
			min: minVal;
			max: maxVal;
			run
]
