ConfigurationOfProjectToolBox
version132ProjectToolBox: spec
	<version: '1.3.2-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'. ].

	spec for: #'level1_1' do: [
		spec package: 'GoferBeau' with: 'GoferBeau-dkh.15'.

		spec for: #'level2_1' do: [
			spec package: 'GoferBeau' with: 'GoferBeau-dkh.25'.

			spec for: #'level3_1' do: [
				spec package: 'GoferBeau' with: 'GoferBeau-dkh.53'. ]. ].

		spec for: #'level2_2' do: [
			spec package: 'GoferBeau' with: 'GoferBeau-dkh.54'.

			spec for: #'level3_2' do: [
				spec package: 'GoferBeau' with: 'GoferBeau-dkh.55'. ]. ]. ].
