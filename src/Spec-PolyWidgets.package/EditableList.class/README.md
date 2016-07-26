This widget allows you to edit a list of items :
- add / remove an item to/from the list 
- order the list by moving elements up/down/top/bottom.

The default behavior is to do a copy of the list. The widget works with its internal copy. It allows the user to accept / reject changes (for example by opening the widget in a DialogWindow) before affecting the original list. It is your responsability to copy EditableList items back to the original list.

The addItemBlock is used to provide a way to give the item to add (e.g. a UIManager default chooseFrom: values:).

Example:
self example