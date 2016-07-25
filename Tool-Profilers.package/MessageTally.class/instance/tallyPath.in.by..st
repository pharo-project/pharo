tallying
tallyPath: context in: aProcess by: count
	| aMethod path |
	aMethod := context method.
	
	"Find the correct child (if there)"
	receivers do: [ :oldTally | 
		(oldTally method == aMethod and: [oldTally process == aProcess])
			ifTrue: [path := oldTally]].
		
	"Add new child if needed"
	path ifNil:[
		path := self class new class: context receiver class method: aMethod;
			process: aProcess;
			reportOtherProcesses: reportOtherProcesses;
			maxClassNameSize: maxClassNameSize;
			maxClassPlusSelectorSize: maxClassPlusSelectorSize;
			maxTabs: maxTabs.
		receivers := receivers copyWith: path].

	^ path bumpBy: count