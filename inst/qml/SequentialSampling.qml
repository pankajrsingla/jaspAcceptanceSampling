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
import "./common" as Common
import JASP

Form
{
	columns: 1
	id: sequential
	property string segment: "Seq"
	IntegerField { name: "lotSize" + sequential.segment; label: qsTr("Lot size (N)"); defaultValue: 1000; min: 1}
    IntegerField { name: "max_n" + sequential.segment; label: qsTr("Max sample size (n)"); defaultValue: 100; min: 1; max: 1000}
	Common.RiskPoints { suffix: sequential.segment }
	// Todo: Use ProbDefect.qml instead of this code. Adjust the minimum values.
	Group
	{
		title: qsTr("Proportion non-conforming items")
		DropDown
		{
			id: 							pd_unit
			name: 							"pd_unit" + sequential.segment
			currentIndex:					0
			label: 							qsTr("Unit for quality levels")
			property var maxPd: (pd_unit.value === "percent") ? 100 : ((pd_unit.value === "proportion") ? 1 : 1000000)
			values:
			[			
				{ label: qsTr("Proportion non-conforming"), 	value: "proportion"},
				{ label: qsTr("Percent non-conforming"),		value: "percent"},
				{ label: qsTr("Non-conforming items per million"), 		value: "per_million"}
			]
		}

		Group
		{
			columns: 2
			Text { text: qsTr("From") }
			DoubleField{ name: "pd_lower" + sequential.segment; label: ""; negativeValues: false; defaultValue: 0.01; min: 0; max: pd_unit.maxPd; decimals: 6; inclusive: JASP.MaxOnly; fieldWidth: 60 }
			Text { text: qsTr("To") }
			DoubleField{ name: "pd_upper" + sequential.segment; label: ""; negativeValues: false; defaultValue: pd_unit.maxPd; min: 0; max: pd_unit.maxPd; decimals: 6; fieldWidth: 60 }
			Text { text: qsTr("Step size") }
			DoubleField{ name: "pd_step" + sequential.segment; label: ""; negativeValues: false; defaultValue: pd_unit.maxPd/10; min: 0; max: pd_unit.maxPd; decimals: 6; fieldWidth: 60 }
		}
	}
	// Common.Distribution {}
	Group
	{
		CheckBox { name: "showPlans" + sequential.segment; label: qsTr("Sequential Plans") }
		Common.OutputOptions { suffix: sequential.segment }
	}
}