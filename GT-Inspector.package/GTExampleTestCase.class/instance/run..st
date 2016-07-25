private
run: aResult
	result := aResult.
	[ super run: aResult ] ensure: [ result := nil ]