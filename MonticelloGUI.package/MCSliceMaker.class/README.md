MCSliceMaker is a dialog to help you making slices.
It allows you to enter the slice number, the summary and to directly select dependent dirty packages.
Then a slice package is added for you in the working copy browser from which it is opened.
Just copy-paste issue number and summary. All is formated for you.

Instance Variables
	info:		<MCSliceInfo>
	okToDoSlice:		<Boolean>
	window:		<StandardWindow>

info
	- It is the model for the user interface

okToDoSlice
	- true if the OK button is clicked, it is to avoid slice making in case of cancel and in the case where the close button of the window has been clicked

window
	- my window
