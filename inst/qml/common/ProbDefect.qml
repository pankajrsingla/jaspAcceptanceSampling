//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program. If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0

Group
{
	title: qsTr("Proportion Non-conforming Items")
	property string suffix: ""
	
	DropDown
	{
		id: 							pd_unit
		name: 							"pd_unit" + suffix
		currentIndex:					0
		label: 							qsTr("Unit for quality levels")
		property var maxPd: (pd_unit.value === "percent") ? 100 : ((pd_unit.value === "proportion") ? 1 : 1000000)
		values:
		[			
			{ label: qsTr("Proportion non-conforming"), 	value: "proportion"},
			{ label: qsTr("Percent non-conforming"),		value: "percent"},
			{ label: qsTr("Non-conforming items per million"), 		value: "per_million"}
		]
		onValueChanged: {
			pd_lower.value = pd_lower.defaultValue
			pd_upper.value = pd_upper.defaultValue
			pd_step.value = pd_step.defaultValue			
		}
	}	
	
	Group
	{
		columns: 2
		Text { text: qsTr("From") }
		DoubleField{ id: pd_lower; name: "pd_lower" + suffix; label: ""; negativeValues: false; defaultValue: 0; min: 0; max: pd_unit.maxPd; decimals: 6; fieldWidth: 60 }
		Text { text: qsTr("To") }
		DoubleField{ id: pd_upper; name: "pd_upper" + suffix; label: ""; negativeValues: false; defaultValue: pd_unit.maxPd; min: 0; max: pd_unit.maxPd; decimals: 6; fieldWidth: 60 }
		Text { text: qsTr("Step size") }
		DoubleField{ id: pd_step; name: "pd_step" + suffix; label: ""; negativeValues: false; defaultValue: pd_unit.maxPd/10; min: 0; max: pd_unit.maxPd; decimals: 6; fieldWidth: 60 }
	}
}
