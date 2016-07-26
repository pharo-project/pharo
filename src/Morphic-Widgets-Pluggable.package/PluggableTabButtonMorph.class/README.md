This is a specialized pluggable button morph that is meant to represent a tab in a set of tabs arranged horizontally.  Each tab will overlap slightly when drawn.  All but one tab will be drawn in left to right order in the specified color, but lighter.  The active tab will be drawn last in the full color and slightly taller to indicate that it is selected.  Clicking the active tab has no effect but clicking any other tab will change the active tab to the clicked tab.

This morph does not itself accept any events.  The parent tab set will grab the mouse clicks and handle notifying the appropriate tabs that they have been activated or deactivated.

There is a single selector which provides the text for the button label and affects the width of the tab.  When the width changes the tab will inform its parent that it has changed and that the layout needs to be updated.  The model for the text selector of course should be the client for the tab set.

The button label can be a String, Text, or Morph.  Texts work better than plain Strings.