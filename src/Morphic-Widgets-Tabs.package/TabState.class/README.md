I represent the current state of a tab.

This class is abstract so have a look at my subclasses to have a better overview.

I have tree responsibilities:
	- influence the rendering (color, border)
	- point out if the state changement asked on my tab is relevant (trying to enable an enabled tab by example)
	- provide the next state for each change (disable, enable, selected)