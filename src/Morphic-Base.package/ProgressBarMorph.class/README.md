Instances of this morph get used by SystemProgressMorph or a JobProgressBar to display a bar (i.e., the rectangular part of a progress bar).

Here is an example of how to use it. 

| p |
p := ProgressBarMorph from: 0 to: 200.
p extent: 200@20.
p openInWorld.

[
	(1 to: 200) do: [ :i | p value: i.  (Delay forMilliseconds: 10) wait ].
	p delete ] fork


| p |
p := ProgressBarMorph from: 0 to: 200.
p extent: 600@7.
p openInWorld.

[
	(1 to: 200) do: [ :i | p value: i.  (Delay forMilliseconds: 5) wait ].
	p delete ] fork