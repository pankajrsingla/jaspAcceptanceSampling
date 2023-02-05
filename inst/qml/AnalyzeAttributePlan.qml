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

Form
{
	Section
	{
		title: qsTr("Single Sampling Plan")
		columns: 1
		// property string analysisSingle: "AnalyzeAttrSingle"

		Common.PlanSingle { suffix: "AnalyzeAttrSingle" }
		Common.Distribution { suffix: "AnalyzeAttrSingle" }

		Group
		{
			CheckBox { name: "assessPlan" + "AnalyzeAttrSingle"; label: qsTr("Assess attribute plan"); id: assessSingle }
			Common.RiskPoints
			{
				suffix: "AnalyzeAttrSingle"
				enabled: assessSingle.checked
			}
		}

		Common.ProbDefect { suffix: "AnalyzeAttrSingle" }
		Common.OutputOptions { suffix: "AnalyzeAttrSingle" }
	}

	Section
	{
		title: qsTr("Multiple Sampling Plan")
		columns: 1
		// property string analysisMult: "AnalyzeAttrMult"

		Common.PlanMultiple { suffix: "AnalyzeAttrMult" }
		Common.Distribution { suffix: "AnalyzeAttrMult" }

		Group
		{
			CheckBox { name: "assessPlan" + "AnalyzeAttrMult"; label: qsTr("Assess sampling plan"); id: assessMult }
			Common.RiskPoints
			{
				suffix: "AnalyzeAttrMult"
				enabled: assessMult.checked
			}
		}

		Common.ProbDefect { suffix: "AnalyzeAttrMult" }		
		Common.OutputOptions { suffix: "AnalyzeAttrMult" }		
	}
}
