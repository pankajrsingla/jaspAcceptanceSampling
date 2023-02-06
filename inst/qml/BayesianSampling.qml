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
	Group
	{
		IntegerField { name: "max_n"; label: qsTr("(Max) Sample size (n)"); defaultValue: 40; min: 1; max: 1000}
		DoubleField { name: "min_bf"; label: qsTr("(Min) Bayes factor (BF)"); defaultValue: 30; min: 1/1000; max: 1000}
	}
	RadioButtonGroup
	{
		title: qsTr("Prior Distribution (Beta)")
		name: "prior"
		RadioButton { value: "impartial"; label: qsTr("Impartial"); checked: true }
		RadioButton { value: "uniform"; label: qsTr("Uniform") }
		Group
		{
			RadioButton { id: custom; value: "custom"; label: qsTr("Custom") }
			Group
			{
				DoubleField{ name: "alpha"; label: qsTr("\u03B1"); defaultValue: 1; min: 0; enabled: custom.checked }
				DoubleField { name: "beta"; label: qsTr("\u03B2"); defaultValue: 1; min: 0; enabled: custom.checked }
			}
		}
	}
	
	Group
	{
		title: qsTr("Quality constraints")
		property bool include_limits: true
		Group
		{
			DoubleField{ name: "aql"; label: qsTr("Acceptable Quality Level (AQL)"); defaultValue: 0.05; min: 0; max: 1; inclusive: JASP.MaxOnly; decimals: 6 }
			DoubleField { name: "rql"; label: qsTr("Rejectable Quality Level (RQL / LTPD)"); defaultValue: 0.15; min: 0; max: 1; inclusive: JASP.MaxOnly; decimals: 6 }
		}		
	}

	Group
	{
        title: qsTr("Output options")
        CheckBox { name: "showPlans"; label: qsTr("Plans table") }
        CheckBox { name: "ncCurve"; label: qsTr("Plans plot") }
		CheckBox { name: "bfCurve"; label: qsTr("BF plot") }
	}
}