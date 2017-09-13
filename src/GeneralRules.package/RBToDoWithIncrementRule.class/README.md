Checks for users of to:do: that also increment or decrement a counter.
In Pharo you do not increment or decrement counter but should use the message to:do:by:.

1 to: 100 by: 3 do: [ :each | ... ]