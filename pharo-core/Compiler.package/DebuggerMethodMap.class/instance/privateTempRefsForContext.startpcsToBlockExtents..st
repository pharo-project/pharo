private
privateTempRefsForContext: aContext startpcsToBlockExtents: theContextsStartpcsToBlockExtents
	"Answer the sequence of temps in scope in aContext in the natural order,
	 outermost arguments and temporaries first, innermost last.  Each temp is
	 a pair of the temp's name followed by a reference.  The reference can be
		integer - index of temp in aContext
		#( indirectionVectorIndex tempIndex ) - remote temp in indirectionVector at index in aContext
		#( outer. temp reference ) - a temp reference in an outer context."
	blockExtentsToTempRefs ifNil:
		[blockExtentsToTempRefs := methodNode blockExtentsToTempsMap.
		 startpcsToTempRefs := Dictionary new].
	^startpcsToTempRefs
		at: aContext startpc
		ifAbsentPut:
			[| localRefs |
			 localRefs := blockExtentsToTempRefs at: (theContextsStartpcsToBlockExtents at: aContext startpc).
			 aContext outerContext
				ifNil: [localRefs]
				ifNotNil:
					[:outer| | outerTemps |
					"Present temps in the order outermost to innermost left-to-right, but replace
					 copied outermost temps with their innermost copies"
					 outerTemps := (self
										privateTempRefsForContext: outer
										startpcsToBlockExtents: theContextsStartpcsToBlockExtents) collect:
						[:outerPair|
						localRefs
							detect: [:localPair| outerPair first = localPair first]
							ifNone: [{ outerPair first. { #outer. outerPair last } }]].
					outerTemps,
					 (localRefs reject: [:localPair| outerTemps anySatisfy: [:outerPair| localPair first = outerPair first]])]]