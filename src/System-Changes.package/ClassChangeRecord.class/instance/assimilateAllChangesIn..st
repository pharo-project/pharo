all changes
assimilateAllChangesIn: otherRecord

	
	otherRecord isClassRemoval ifTrue: [^ self noteChangeType: #remove].

	otherRecord allChangeTypes do:
		[:chg | self noteChangeType: chg fromClass: self realClass].

	otherRecord methodChanges associationsDo:
		[:assn | | changeType selector changeRecord | selector := assn key. changeRecord := assn value.
		changeType := changeRecord changeType.
		(changeType == #remove or: [changeType == #addedThenRemoved])
			ifTrue:
				[changeType == #addedThenRemoved
					ifTrue: [self atSelector: selector put: #add].
				self noteRemoveSelector: selector priorMethod: nil
						lastMethodInfo: changeRecord methodInfoFromRemoval]
			ifFalse: 
				[self atSelector: selector put: changeType]].
