gui
showCurrentLesson
	| progressInfo lesson |
	lesson := self player currentLesson.
	progressInfo := '(', self tutorialPositionString, '/', self tutorialSizeString, ')'. 
	^ self lessonView 
		showLesson: lesson 
		withTitle: lesson title, ' ', progressInfo.