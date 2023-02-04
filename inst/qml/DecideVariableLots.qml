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
	property string analysis: "DecideVar"
	VariablesForm
	{
		AvailableVariablesList	{ name: "allVariablesList" + analysis; id: allVariablesList }
		AssignedVariablesList	{ name: "variables" + analysis; title: qsTr("Measurement"); id: variables; singleVariable: true; suggestedColumns: ["scale"]; allowedColumns: ["scale"] }
	}

	Group
	{
		enabled: variables.count != 1
		Text { text: qsTr("Specify sample statistics directly (used if dataset is not available)") }
		CheckBox { name: "sampleStats" + analysis; id: sampleStats; checked: (variables.count != 1); enabled: false; visible: false }
		IntegerField { name: "sampleSize" + analysis; label: qsTr("Sample size (n)"); defaultValue: 24; min: 1 }
		DoubleField { name: "sampleMean" + analysis; label: qsTr("Sample mean"); defaultValue: 1.5; decimals: 6 }
		DoubleField { name: "sampleSD" + analysis; label: qsTr("Sample standard deviation"); defaultValue: 1; min: 0; inclusive: JASP.None; decimals: 6 }
	}

	// Todo: Label for k is a hacky solution to align it with the sample values. Adjust the width in code.
	DoubleField { name: "kValue" + analysis; label: qsTr("k                        "); defaultValue: 1.309; min: 0; negativeValues: false; inclusive: JASP.None; decimals: 6 }
	
	Group
	{
		title: qsTr("Specification limits")
		columns: 2
		CheckBox { name: "lsl" + analysis; label: qsTr("Lower Specification Limit (LSL)"); id: lsl; checked: false }
		DoubleField{ name: "lower_spec" + analysis; label: ""; id: lower_spec; defaultValue: 0; enabled: lsl.checked; negativeValues: true; max: upper_spec.value; inclusive: JASP.MaxOnly; decimals: 6 }
		CheckBox { name: "usl" + analysis; label: qsTr("Upper Specification Limit (USL)"); id: usl; checked: false }
		DoubleField { name: "upper_spec" + analysis; label: ""; id: upper_spec; defaultValue: 1; enabled: usl.checked; negativeValues: true; min: lower_spec.value; inclusive: JASP.MinOnly; decimals: 6 }
	}

	Group
	{
		columns: 2
		CheckBox { name: "sd" + analysis; label: qsTr("Standard deviation (historical)"); id: sd; checked: false }
		DoubleField { name: "stdev" + analysis; label: ""; enabled: sd.checked; defaultValue: 1; min: 0; negativeValues: false; inclusive: JASP.None; decimals: 6 }
	}

	Group
	{
		title: qsTr("Quality constraints")
		enabled: lsl.checked && usl.checked && sd.checked
		columns: 2
		Text { text: qsTr("Acceptable Quality Level (AQL)") }
		DoubleField{ name: "aql" + analysis; label: ""; negativeValues: false; defaultValue: 0.05; min: 0; max: 1; inclusive: JASP.None; decimals: 6 }
		Text { text: qsTr("Rejectable Quality Level (RQL / LTPD)") }
		DoubleField { name: "rql" + analysis; label: ""; negativeValues: false; defaultValue: 0.15; min: 0; max: 1; inclusive: JASP.None; decimals: 6 }
	}
}
