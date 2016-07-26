menu messages
highlightAndEmptyCheck: returnBlock
	| selectionInterval |

	selectionInterval := self getHighlightInterval.
	selectionInterval ifEmpty: [ textArea flash.  ^ returnBlock value ].
	self highlightInterval: selectionInterval