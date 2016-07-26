menus
copyListToClipboard
	"Copy my items to the clipboard as a multi-line string"

	| stream |
	stream := (String new: self getList size * 40) writeStream.
	list do: [:ea | stream nextPutAll: ea asString] separatedBy: [stream nextPut: Character cr].
	Clipboard clipboardText: stream contents