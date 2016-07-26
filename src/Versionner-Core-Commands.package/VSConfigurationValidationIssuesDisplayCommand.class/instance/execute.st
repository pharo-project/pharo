execute
execute
	| list |

	list := ListModel new.
	list openWithSpec.
	list title: target messageText.
	list items: target issues.