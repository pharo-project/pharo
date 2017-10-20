I am a group of menu items.

I am part of a menu (MenuPresenter), and groups items (MenuItemPresenter) by meaning.

You can also have a look at ApplicationWithToolbar for a full example of how to use them.

I provide the following methods
- addItem: use the block in parameter to initialize a new item, then add it in my collection.
- addMenuItem: add the item in parameter in my collection.
- isEmpty return a boolean representing if my collection is empty.
- menuItems getter of my collection.


todo
- autoRefresh
- buildWithSpecLayout:
- fromSpec: