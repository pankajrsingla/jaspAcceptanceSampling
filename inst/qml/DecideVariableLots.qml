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
	VariablesForm
	{
		AvailableVariablesList	{ name: "allVariablesList"; id: allVariablesList }
		AssignedVariablesList	{ name: "variables"; title: qsTr("Measurement"); id: variables; singleVariable: true; suggestedColumns: ["scale"]; allowedColumns: ["scale"] }
	}

	Group
	{
		enabled: variables.count != 1
		CheckBox { name: "sampleStats"; label: qsTr("Specify sample statistics directly (used if dataset is not available)"); id: sampleStats; checked: (variables.count != 1); enabled: false }
		IntegerField { name: "sampleSize"; label: qsTr("Sample size (n)"); defaultValue: 24; min: 1 }
		DoubleField { name: "sampleMean"; label: qsTr("Sample mean"); defaultValue: 1.5 }
		DoubleField { name: "sampleSD"; label: qsTr("Sample standard deviation"); defaultValue: 1; min: 0; inclusive: JASP.None }
	}

	// Todo: Label for k is a hacky solution to align it with the sample values. Adjust the width in code.
	DoubleField { name: "kValue"; label: qsTr("k                        "); defaultValue: 1.309; min: 0; negativeValues: false; inclusive: JASP.None }
	
	// Todo: Decide if more than 3 decimals are to be allowed for LSL and USL specification. Right now, you can't.
	Group
	{
		title: qsTr("Specification limits")
		columns: 2
		CheckBox { name: "lsl"; label: qsTr("Lower Specification Limit (LSL)"); id: lsl; checked: false }
		DoubleField{ name: "lower_spec"; label: qsTr(""); defaultValue: 0; enabled: lsl.checked; negativeValues: true }
		CheckBox { name: "usl"; label: qsTr("Upper Specification Limit (USL)"); id: usl; checked: false }
		DoubleField { name: "upper_spec"; label: qsTr(""); enabled: usl.checked; negativeValues: true }
	}

	Group
	{
		columns: 2
		CheckBox { name: "sd"; label: qsTr("Standard Deviation (Historical)"); id: sd; checked: false }
		DoubleField { name: "stdev"; label: qsTr(""); enabled: sd.checked; defaultValue: 1; min: 0; negativeValues: false; inclusive: JASP.None }
	}

	Group
    {
		title: qsTr("Quality constraints")
		enabled: lsl.checked && usl.checked && sd.checked
		columns: 2
        Text { text: qsTr("Acceptable Quality Level (AQL)") }
        DoubleField{ name: "aql"; label: qsTr(""); negativeValues: false; defaultValue: 0.05; min: 0; max: 1; inclusive: JASP.None }
        Text { text: qsTr("Rejectable Quality Level (RQL / LTPD)") }
        DoubleField { name: "rql"; label: qsTr(""); negativeValues: false; defaultValue: 0.15; min: 0; max: 1; inclusive: JASP.None }
	}
}
