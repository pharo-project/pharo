text
buildClassDescriptionFor: changeSet class: class
	| stream |

	stream := (String new: 100) writeStream.
		(changeSet classChangeAt: class name)
			do: [:each | 
				each = #remove
					ifTrue: [ stream nextPutAll: 'Entire class was removed.'; cr ].
				each = #addedThenRemoved
					ifTrue: [ stream nextPutAll: 'Class was added then removed.'; cr ].
				each = #rename
					ifTrue: [ stream nextPutAll: 'Class name was changed.'; cr ].
				each = #add
					ifTrue: [ stream nextPutAll: 'Class definition was added.'; cr ].
				each = #change
					ifTrue: [ stream nextPutAll: 'Class definition was changed.'; cr ].
				each = #reorganize
					ifTrue: [ stream nextPutAll: 'Class organization was changed.'; cr ].
				each = #comment 
					ifTrue: [ stream nextPutAll: 'New class comment.'; cr ]].
		^ stream contents