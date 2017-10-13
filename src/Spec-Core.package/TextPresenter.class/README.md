I handle basic multi-line text.
See AbstractWidgetPresenter

self example

You can also have a look at ListSelectionPresenter and ScrollSyncExample for more examples.

My main purpose is to handle source code (I have editing shortcuts support).

I provide the following variables and their accessors
- autoAccept is a boolean representing if the text is accepted on each keystroke.
- text is the text of the text zone.

I provide the following methods
- beForCode and beForText are shortcut for configuring me.
- getSelection return the interval currently selected.
- getText is a getter of the text


todo
- aboutToStyleBlock  is the block used to know if the text must be styled, it returns a boolean. aboutToStyle: is a shortcut to set directly the boolean.
- accept , accept:notifying:
- acceptBlock should convert the text in a boolean representing if it is to be accepted.
- askBeforeDiscardingEdits
- behavior
- clearSelection
- codePaneMenu:shifted:
- doItContext
- doItReceiver
- specializing eventKeyStrokesForNextFocus , eventKeyStrokesForPreviousFocus
- getMenu
- hasEditingConflicts
- hasUnacceptedEdits
- isAboutToStyle
- isCodeCompletionAllowed
- isForSmalltalkCode
- menuHolder
- notify:at:in:
- readSelection , readSelectionBlock
- registerEvents , registerEventsForShout
- scrollValue
- selectedBehavior
- selectedClassOrMetaClass
-  setSelection: select the text in the interval in argument, selectAll is a shorcut that set the larger interval.
- setSelectionInterval:
- textArea
