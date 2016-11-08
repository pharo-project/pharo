comparing
match: aString ifScored: aBlock
	
	| score |
	
	score := 0.	
		
	pattern isEmpty ifTrue: [ aBlock value: score. ^ self ].
	
	(self matches: aString) ifFalse: [ ^ self ].
	
	score := self firstScore: aString at: indexes first.
	
	2 to: pattern size do: [ :pix | 
		score := score + (self score: aString at: (indexes at: pix) patternAt: pix).
		
	].
		
	score := score + self indexScore + ((aString size - pattern size) * self unmatchedLetterPenalty).
		
	aBlock value: score.	