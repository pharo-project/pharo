testing
isEditEvent: anEvent
	"Answer whether the given event is a key stroke that
	should be allowed despite the max length."

	|key|
	key := anEvent keyValue asCharacter.
	^key == Character cr or: [
		key == Character escape or: [
		key == Character backspace or: [
		key == Character delete or: [
		key == Character arrowLeft or: [
		key == Character arrowRight or: [
		key == Character home or: [
		key == Character end or: [
			anEvent commandKeyPressed and: [key = $a]]]]]]]]]