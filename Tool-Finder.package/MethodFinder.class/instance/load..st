public api
load: dataWithAnswers
	"Find a function that takes the data and gives the answers.  Odd list entries are data for it, even ones are the answers.  nil input means data and answers were supplied already."
	"(MethodFinder new) load: #( (4 3) 7  (-10 5) -5  (-3 11) 8);
		findMessage  "

	dataWithAnswers ifNotNil: [
		data := Array new: dataWithAnswers size // 2.
		1 to: data size do: [:ii | data at: ii put: (dataWithAnswers at: ii*2-1)].
		answers := Array new: data size.
		1 to: answers size do: [:ii | answers at: ii put: (dataWithAnswers at: ii*2)]].
	data do: [:list | 
		(list isKindOf: SequenceableCollection) ifFalse: [
			^ self error: 'first and third items are not Arrays'].
		].
	argMap := (1 to: data first size) asArray.
	data do: [:list | list size = argMap size ifFalse: [
			self error: 'data arrays must all be the same size']].
	argMap size > 4 ifTrue: [self error: 'No more than a receiver and 
		three arguments allowed'].
		
	thisData := data copy.
	mapStage := mapList := nil.
