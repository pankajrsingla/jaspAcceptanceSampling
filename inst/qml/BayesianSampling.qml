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
	Section
	{
		title: qsTr("Planning")
		columns: 1
		Group
		{
			IntegerField { name: "max_n"; label: qsTr("(Max) Sample size (n)"); defaultValue: 40; min: 1; max: 1000 }
			DoubleField { name: "min_bf"; label: qsTr("(Min) Bayes factor (BF)"); defaultValue: 30; min: 1/1000; max: 1000 }
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
			CheckBox { name: "priorCurve"; label: qsTr("Prior plot") }
			CheckBox { name: "ncCurve"; label: qsTr("Plans plot") }
			CheckBox { name: "bfCurve"; label: qsTr("BF plot") }
		}
	}
	Section
	{
		title: qsTr("Inference")
		columns: 1
		Group
		{
			RadioButtonGroup
			{
				title: qsTr("Prior")
				name: "prior_inf"
				RadioButton { value: "usePrevPrior"; label: qsTr("Use prior from planning"); checked: true }
				RadioButton { id: useNewPrior; value: "useNewPrior"; label: qsTr("Specify new prior (Beta)") }
				RadioButtonGroup
				{
					name: "new_prior"
					enabled: useNewPrior.checked
					RadioButton { value: "impartial_inf"; label: qsTr("Impartial"); checked: true }
					RadioButton { value: "uniform_inf"; label: qsTr("Uniform") }
					Group
					{
						RadioButton { id: new_custom; value: "new_custom"; label: qsTr("Custom") }
						Group
						{
							DoubleField{ name: "alpha_inf"; label: qsTr("\u03B1"); defaultValue: 1; min: 0; enabled: new_custom.checked }
							DoubleField { name: "beta_inf"; label: qsTr("\u03B2"); defaultValue: 1; min: 0; enabled: new_custom.checked }
						}
					}
				}
				Group
				{
					title: qsTr("Quality constraints")
					enabled: useNewPrior.checked
					Group
					{
						DoubleField{ name: "aql_inf"; label: qsTr("Acceptable Quality Level (AQL)"); defaultValue: 0.05; min: 0; max: 1; inclusive: JASP.MaxOnly; decimals: 6 }
						DoubleField { name: "rql_inf"; label: qsTr("Rejectable Quality Level (RQL / LTPD)"); defaultValue: 0.15; min: 0; max: 1; inclusive: JASP.MaxOnly; decimals: 6 }
					}		
				}				
			}
		}
		Group
		{
			title: qsTr("Specify data")
			Group
			{
				IntegerField { name: "data_n"; id: data_n; label: qsTr("Sample size (n)"); defaultValue: 40; min: 1; max: 1000 }
				IntegerField { name: "data_c"; label: qsTr("Acceptance number (c)"); defaultValue: 1; min: 0; max: parseInt(data_n.value) }
				IntegerField { name: "data_d"; label: qsTr("(Max) Sample size (n)"); defaultValue: 40; min: 1; max: parseInt(data_n.value) }
			}
		}
		Group
		{
			title: qsTr("Output options")
			CheckBox { name: "inf_priorCurve"; label: qsTr("Prior plot") }
			CheckBox { name: "inf_posteriorCurve"; label: qsTr("Posterior plot") }			
		}
	}
}