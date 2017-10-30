I represent a menu item.

You can also have a look at ApplicationWithToolbar for a full example of how to use them.

I collaborate with MenuGroupPresenter to manage me.

I provide the following variables and their accessors
- name is the text I display.
- icon is the text I display.
- description is the text I display in my tooltip.
- action is the action to perform when I am clicked.
- subMenu is the submenu to display when I am clicked.

Usually I do NOT have an action AND a submenu.

todo
- autoRefresh
- enabled
- fromSpec:
- performMenuActionWith:
- shortcut is ..., the shortcut is displayed near my name.
- state