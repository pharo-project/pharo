
[

| info lines dict unimplemented sends unsent o kernelPackages networkPackages 
goferPackages pkgs allImplementedMessagesWithout allSentMessagesWithout |

SmalltalkImage current fixObsoleteReferences.
Smalltalk garbageCollect.
Smalltalk garbageCollect.
Smalltalk garbageCollect.

info := FileStream forceNewFileNamed: 'info.txt'.

Undeclared removeUnreferencedKeys.

info nextPutAll: 'Undeclared:'; cr.
info nextPutAll: Undeclared asString; cr; cr.

info nextPutAll: 'Undeclared sorted by name:'; cr.



info nextPutAll: 'Obsolete:'; cr.
info nextPutAll: SystemNavigation default obsoleteClasses asString; cr; cr.

info nextPutAll: 'Unimplemented calls size:'; space.
info nextPutAll: SystemNavigation default allUnimplementedCalls size asString.

info cr; cr.
info nextPutAll: 'Unimplemented calls:'; cr; cr.

unimplemented := SystemNavigation default allUnimplementedCalls value asSortedCollection.

sends := OrderedCollection new.
unimplemented do: [:call |
  | pos |
  pos := (call findString:  ' calls: ').
  ((call allButFirst: (pos + 7)) findTokens: ' ') do: [:send |
    sends add: (send asString, ' calledBy: ', (call first: pos))].
  info nextPutAll: call asString; cr.].
info cr.
info cr.
sends asSortedCollection do: [:call |
  info nextPutAll: call asString; cr.].
  
info cr; nextPutAll: 'finished...'.

info close.


SmalltalkImage current snapshot: false andQuit: true.

] ifError: [:e |
	| rep |
	rep := FileStream forceNewFileNamed: 'PharoDebug.log'.
	rep nextPutAll: 'info.st'; cr.
	rep nextPutAll: 'Error:'; cr.
	rep nextPutAll: e asString; cr.
	rep nextPutAll: thisContext stack size asString.
	thisContext stack copy withIndexDo: [:stck :i |
		[rep nextPutAll: i asString; space; nextPutAll: stck asString; cr] ifError: []].
	rep close. 
	Smalltalk exitFailure] 


