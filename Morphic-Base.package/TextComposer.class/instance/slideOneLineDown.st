private
slideOneLineDown

	| priorLine |

	"Having detected the end of rippling recoposition, we are only sliding old lines"
	prevIndex < prevLines size ifFalse: [
		"There are no more prevLines to slide."
		^nowSliding := possibleSlide := false
	].

	"Adjust and re-use previously composed line"
	prevIndex := prevIndex + 1.
	priorLine := (prevLines at: prevIndex)
				slideIndexBy: deltaCharIndex andMoveTopTo: currentY.
	lines addLast: priorLine.
	currentY := priorLine bottom.
	currCharIndex := priorLine last + 1.
	wantsColumnBreaks ifTrue: [
		priorLine first to: priorLine last do: [ :i |
			(theText at: i) = TextComposer characterForColumnBreak ifTrue: [
				nowSliding := possibleSlide := false.
				^nil
			].
		].
	].
