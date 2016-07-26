special filters
isMethod: aMethod

	^ self
		title: 'is ', aMethod printString
		condition: [ :e | 
			e content isEpMethodChange and: [ 
			e content behaviorAffectedName = aMethod methodClass name and: [ 
			e content methodAffectedSelector = aMethod selector ] ] ]