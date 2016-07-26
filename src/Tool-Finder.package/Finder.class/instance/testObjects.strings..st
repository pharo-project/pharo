private
testObjects: dataObjects strings: dataStrings
	"Try to make substitutions in the user's inputs and search for the selector again.
		1 no change to answer.
		2 answer Array -> OrderedCollection.
		2 answer Character -> String
		4 answer Symbol or String of len 1 -> Character
	For each of these, try straight, and try converting args:
		Character -> String
		Symbol or String of len 1 -> Character
	Return array with result, dataObjects, dataStrings.  Don't ever do a find on the same set of data twice."

	| selectors classes didUnmodifiedAnswer |
	
	selectors := {#asString. #first. #asOrderedCollection}.
	classes := {Character. String. Array}.
	didUnmodifiedAnswer := false.
	
	selectors withIndexDo: [:ansSel :ansInd | | tempDataObjects result tempDataStrings answerMod | 
		"Modify the answer object"
		answerMod := false.
		tempDataObjects := dataObjects copyTwoLevel.  
		tempDataStrings := dataStrings copy.
		(dataObjects last isKindOf: (classes at: ansInd)) ifTrue: [
			((ansSel ~~ #first) or: [dataObjects last size = 1]) ifTrue: [
				tempDataObjects at: tempDataObjects size put: (tempDataObjects last perform: ansSel).	"asString"
				tempDataStrings at: tempDataStrings size put: tempDataStrings last, ' ', ansSel.
				result := MethodFinder new load: tempDataObjects; findMessage.
				(result first beginsWith: 'no single method') ifFalse: [
					"found a selector!"
					^ Array with: result first with: tempDataObjects with: tempDataStrings].	
				answerMod := true]].

		selectors allButLast withIndexDo: [:argSel :argInd | | dds ddo | 
			"Modify an argument object"
			"for args, no reason to do Array -> OrderedCollection.  Identical protocol."
			didUnmodifiedAnswer not | answerMod ifTrue: [
			ddo := tempDataObjects copyTwoLevel. 
			dds := tempDataStrings copy.
			dataObjects first withIndexDo: [:arg :ind |
				(arg isKindOf: (classes at: argInd))  ifTrue: [
					((argSel ~~ #first) or: [arg size = 1]) ifTrue: [
						ddo first at: ind put: ((ddo first at: ind) perform: argSel).	"asString"
						dds at: ind put: (dds at: ind), ' ', argSel.
						result := MethodFinder new load: ddo; findMessage.
						(result first beginsWith: 'no single method') ifFalse: [
							"found a selector!"
						^ Array with: result first with: ddo with: dds].	
					didUnmodifiedAnswer not & answerMod not ifTrue: [
						didUnmodifiedAnswer := true].
					]]]]].
	].
	^ Array with: 'no single method does that function' with: dataObjects with: dataStrings