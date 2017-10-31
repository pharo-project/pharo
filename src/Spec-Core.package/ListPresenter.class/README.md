A ListPresenter is an applicative presenter which handle a basic list.
See AbstractWidgetPresenter

self example
self example2
self example3

You can also have a look at ExampleListPresenter >> exampleRegisteredColor and ListSelectionPresenter for more examples.

I provide the following variables and their accessors
- backgroundColorBlock should convert an item in the color in the background in the line of this item.
- displayBlock should convert an item in something that can be displayed in a list, use asStringOrText by default.
- filteringBlock should convert an item in a boolean representing if the item will be displayed, initialize with self defaultFilteringBlock.
- multiSelection is a boolean representing if several items can be selected in the same time, it have shortcut beMultipleSelection and beSingleSelection.
- sortingBlock should convert two items in a boolean representing if the first item must be place before the second, initialize with self defaultSortingBlock.

Blocks should be set before items.

I provide the following methods
- items: to set the list with the elements of the collection in parameter
- getIndex is the getter of the index of the selected item or the last selected item.
- getList is the getter of the list.
- listSize is the number of items.


todo
- allowToSelect
- autoDeselect
- menu
- listItems
- clickOnSelectedItem
- getSelectionStateFor:
- listElementAt:ifAbsent:
- refresh ...
- select ... , setSelected ...
- updateList
