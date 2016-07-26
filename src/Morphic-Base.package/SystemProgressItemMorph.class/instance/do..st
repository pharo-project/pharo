private
do: aBlock

	self changed. "We may be in the UI thread, so this will give us a change to dsiplay ourselves before the block starts"
	^ [ aBlock value: self ] ensure: [ self close ].