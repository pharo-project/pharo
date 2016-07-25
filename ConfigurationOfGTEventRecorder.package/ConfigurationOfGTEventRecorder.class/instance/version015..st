versions
version015: spec
	<version: '0.15' imports: #('0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.20 (case 18022)'.
		spec author: 'AndreiChis'.
		spec timestamp: '4/18/2016 22:57'.
		spec project: 'Ston' with: '0.18'.
		spec 
			package: #'GT-EventRecorder' with: 'GT-EventRecorder-PavelKrivanek.66';
			package: 'GT-EventRecorder-Tests' with: 'GT-EventRecorder-Tests-JurajKubelka.37'. ].
