private
defaultContents
	contents := list isNil
		ifTrue: [String new]
		ifFalse: [
			String streamContents: [:s | 
				s << 'NO FILE SELECTED' translated; cr.
				s << '  -- Folder Summary --' translated; cr.
				list do: [:item | s << item basename; cr]]].
	brevityState := #FileList.
	^ contents