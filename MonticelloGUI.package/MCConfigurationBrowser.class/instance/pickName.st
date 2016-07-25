morphic ui
pickName
	| name |
	name := UIManager default
		request: ('Name' translated, ' (.', self configuration writerClass extension, ' will be appended' translated, '):')
		initialAnswer: (self configuration name ifNil: ['']).
	^ name isEmptyOrNil ifFalse: [name]