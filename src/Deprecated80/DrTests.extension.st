Extension { #name : #DrTests }

{ #category : #'*Deprecated80' }
DrTests >> resultItemSelected [
	self deprecated: 'This method is not used in DrTests and the implementation is strange.
Thus, it will be removed soon.
Do not use it.
You are probably looking for #resultSelected.'.
	^ self resultsList ifNil: [ #() ] ifNotNil: #selectedItem
]

{ #category : #'*Deprecated80' }
DrTests >> resultViewsDropList: anObject [
	self deprecated: 'There is no reason to provide a mutator for this inst. var.
Going to be removed.'.
	resultViewsDropList:= anObject 
]

{ #category : #'*Deprecated80' }
DrTests >> runPluginMethod: aSymbol [
	self deprecated: 'Meta-programming should be removed.'.
	[ self lock.
	self updateStatus: 'Tests started.'.
	self
		updateWithResults: (self currentPlugin perform: aSymbol with: self pluginResult).
	self updateStatus: 'Tests finished.' ]
		ensure: [ self unlock ]
]
