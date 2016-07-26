This is a morph used for displaying various notifications.

It has a special ability to delete itself either when Esc is pressed, or when the focus is lost.

When opened the user has to spefify a text morph whose position and cursor are user to position this morph.

When triggered from a context menu action the class method installAlarmFor: should be used to open the popper as the text morph gets back the focus after the action was executed (and, hence, the popper is closed).