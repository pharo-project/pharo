private
dynamicTab
      | tab |
      tab := self newTab.
      tab
          label: 'Dynamic';
          icon: (tab iconNamed: #nautilusIcon);
          model: (DynamicWidgetChange  new).
      ^ tab 