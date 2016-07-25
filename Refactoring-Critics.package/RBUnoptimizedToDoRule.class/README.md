Checks for inefficient uses of to:do: that create an unnecessary Interval instance.

( 1 to: 10 ) do: aBlock

can be more efficiently expressed as 

1 to: 10 do: aBlock