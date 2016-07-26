class changes
fatDefForClass: class 
	| newDef oldDef oldStrm newStrm outStrm oldVars newVars addedVars |
	class isBehavior ifFalse: [ ^ class definition ].
	newDef := class definition.
	oldDef := (self changeRecorderFor: class) priorDefinition.
	oldDef ifNil: [ ^ newDef ].
	oldDef = newDef ifTrue: [ ^ newDef ].
	oldStrm := oldDef readStream.
	newStrm := newDef readStream.
	outStrm := (String new: newDef size * 2) writeStream.

	"Merge inst vars from old and new defs..."
	oldStrm
		upToAll: 'instanceVariableNames';
		upTo: $'.
	outStrm
		nextPutAll: (newStrm upToAll: 'instanceVariableNames');
		nextPutAll: 'instanceVariableNames:'.
	newStrm peek = $: ifTrue: [ newStrm next ].	"may or may not be there, but already written"
	outStrm
		nextPutAll: (newStrm upTo: $');
		nextPut: $'.
	oldVars := (oldStrm upTo: $') findTokens: Character separators.
	newVars := (newStrm upTo: $') findTokens: Character separators.
	addedVars := oldVars asSet
		addAll: newVars;
		removeAll: oldVars;
		asOrderedCollection.
	oldVars , addedVars do: 
		[ :var | 
		outStrm
			nextPutAll: var;
			space ].
	outStrm nextPut: $'.
	class isMeta ifFalse: 
		[ "Merge class vars from old and new defs..."
		oldStrm
			upToAll: 'classVariableNames:';
			upTo: $'.
		outStrm
			nextPutAll: (newStrm upToAll: 'classVariableNames:');
			nextPutAll: 'classVariableNames:';
			nextPutAll: (newStrm upTo: $');
			nextPut: $'.
		oldVars := (oldStrm upTo: $') findTokens: Character separators.
		newVars := (newStrm upTo: $') findTokens: Character separators.
		addedVars := oldVars asSet
			addAll: newVars;
			removeAll: oldVars;
			asOrderedCollection.
		oldVars , addedVars do: 
			[ :var | 
			outStrm
				nextPutAll: var;
				space ].
		outStrm nextPut: $' ].
	outStrm nextPutAll: newStrm upToEnd.
	^ outStrm contents