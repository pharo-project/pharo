A port that belongs to a pane.

Bound ports have a few special characteristics in comparison to their superclass. For one, they don't just assign a value to themselves using #value: but rather generate a transmission that set the value so that the pane can handle the transmission and forward it to other ports if necessary, depending on the policy of the containing browser.