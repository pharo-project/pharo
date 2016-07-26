initialization
bindingDictionnary
	^ {Character escape -> [ self spotterModel exitDueTo: #keyPressedEscape ].
	$w meta -> [ self spotterModel exitDueTo: #keyPressedW ].
	$p meta -> [ self togglePreview ].
	Character arrowUp -> [ self selectPreviousCandidateDueTo: #keyPressedArrowUp ].
	Character arrowDown -> [ self selectNextCandidateDueTo: #keyPressedArrowDown ].
	Character arrowUp meta -> [ self selectPreviousCandidateDueTo: #keyPressedArrowUp ].
	Character arrowDown meta -> [ self selectNextCandidateDueTo: #keyPressedArrowDown ].
	Character arrowUp shift -> [ self selectPreviousCandidateDueTo: #keyPressedArrowUp ].
	Character arrowDown shift -> [ self selectNextCandidateDueTo: #keyPressedArrowDown ].
	Character arrowUp meta shift -> [ self selectPreviousCategory ].
	Character arrowDown meta shift -> [ self selectNextCategory ].
	Character arrowRight meta -> [ self diveInDueTo: #keyPressed ].
	Character arrowLeft meta -> [ self diveOut ].
	Character arrowRight meta shift -> [ self diveInCategoryDueTo: #keyPressed ].
	Character arrowLeft meta shift -> [ self diveOut ].
	Character arrowUp alt -> [ self stepScrollPageUp ].
	Character arrowDown alt -> [ self stepScrollPageDown ].
	Character pageUp -> [ self stepScrollPageUp ].
	Character pageDown -> [ self stepScrollPageDown ].
	Character cr asKeyCombination -> [ self actDueTo: #keyPressed ].
	$p meta shift -> [ self exportAsPNGSilently ]}