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
		id: plan
		property string segment: "plan"
		Group
		{
			title: qsTr("Quality constraints")
			DoubleField{ name: "aql" + plan.segment; label: qsTr("Acceptable Quality Level (AQL)"); defaultValue: 0.05; min: 0; max: 1; inclusive: JASP.MaxOnly; decimals: 6 }
			DoubleField { name: "rql" + plan.segment; label: qsTr("Rejectable Quality Level (RQL / LTPD)"); defaultValue: 0.15; min: 0; max: 1; inclusive: JASP.MaxOnly; decimals: 6 }			
		}

		Common.PriorDistribution { suffix: plan.segment }

		Group
		{
			IntegerField { name: "max_n" + plan.segment; label: qsTr("(Max) Sample size (n)"); defaultValue: 40; min: 1; max: 1000 }
			DoubleField { name: "min_bf" + plan.segment; label: qsTr("Bayes factor (min)"); defaultValue: 30; min: 1/10000; max: 10000 }
		}				

		Group
		{
			title: qsTr("Output options")
			CheckBox { name: "showPlans" + plan.segment; label: qsTr("Plans table") }
			CheckBox { name: "priorPlot" + plan.segment; label: qsTr("Prior plot") }
			CheckBox { name: "ncPlot" + plan.segment; label: qsTr("Plans plot") }
			CheckBox { name: "bfPlot" + plan.segment; label: qsTr("BF plot") }
		}
	}
	
	Section
	{
		title: qsTr("Inference")
		columns: 1
		id: infer
		property string segment: "infer"
		CheckBox { id: inferPosterior; name: "inferPosterior" + infer.segment; label: qsTr("Use data to create posterior distribution") }		
		Group
		{
			enabled: inferPosterior.checked
			RadioButtonGroup
			{
				title: qsTr("Choose prior")
				name: "choosePrior" + infer.segment
				RadioButton { value: "usePrev"; label: qsTr("Use constraints and prior from planning phase"); checked: true }
				RadioButton 
				{ 
					id: useNewPrior; value: "useNew"; label: qsTr("Use different constraints and prior")
					Group
					{
						title: qsTr("Quality constraints")
						DoubleField{ name: "aql" + infer.segment; label: qsTr("Acceptable Quality Level (AQL)"); defaultValue: 0.05; min: 0; max: 1; inclusive: JASP.MaxOnly; decimals: 6 }
						DoubleField { name: "rql" + infer.segment; label: qsTr("Rejectable Quality Level (RQL / LTPD)"); defaultValue: 0.15; min: 0; max: 1; inclusive: JASP.MaxOnly; decimals: 6 }			
					}
					Common.PriorDistribution { suffix: infer.segment }												
				}
			}
			Group
			{
				title: qsTr("Specify data")
				IntegerField { name: "data_n" + infer.segment; id: data_n; label: qsTr("Sample size (n)"); defaultValue: 40; min: 1; max: 1000 }
				IntegerField { name: "data_d" + infer.segment; label: qsTr("Observed number of defects (d)"); defaultValue: 1; min: 0; max: parseInt(data_n.value) }			
			}
			Group
			{
				title: qsTr("Output options")
				CheckBox { name: "priorPlot" + infer.segment; label: qsTr("Prior plot") }
				CheckBox { name: "posteriorPlot" + infer.segment; label: qsTr("Posterior plot") }			
			}
		}
	}

	Section
	{
		title: qsTr("Update")
		columns: 1
		id: update
		property string segment: "update"
		CheckBox { id: updatePlan; name: "updatePlan" + update.segment; label: qsTr("Use posterior data to generate new plans") }
		Group
		{
			enabled: updatePlan.checked
			DoubleField { name: "min_bf" + update.segment; label: qsTr("Bayes factor (min)"); defaultValue: 30; min: 1/10000; max: 10000 }
			Group
			{
				title: qsTr("Output options")
				CheckBox { name: "showPlans" + update.segment; label: qsTr("Plans table") }
				CheckBox { name: "priorPlot" + update.segment; label: qsTr("Updated prior plot") }
				CheckBox { name: "ncPlot" + update.segment; label: qsTr("Plans plot") }
				CheckBox { name: "bfPlot" + update.segment; label: qsTr("BF plot") }
			}
		}
	}
}