world menu
menuCommandOn: aBuilder 
	<worldMenu>
	(aBuilder item: #'Time Profiler')
		parent: #Tools;  
		order: 0.40;
		icon: self taskbarIcon;
		action: [TimeProfiler new open]