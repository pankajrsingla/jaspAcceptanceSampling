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
		id: analyzeAttrSingle
		property string segment: "AnalyzeAttrSingle"

		Common.PlanSingle { suffix: analyzeAttrSingle.segment }
		Common.Distribution { suffix: analyzeAttrSingle.segment }

		Group
		{
			CheckBox { name: "assessPlan" + analyzeAttrSingle.segment; label: qsTr("Assess Plan"); id: assessSingle }
			Common.RiskPoints
			{
				suffix: analyzeAttrSingle.segment
				enabled: assessSingle.checked
			}
		}

		Common.ProbDefect { suffix: analyzeAttrSingle.segment }
		Common.OutputOptions { suffix: analyzeAttrSingle.segment }
	}

	Section
	{
		title: qsTr("Multiple Sampling Plan")
		columns: 1
		id: analyzeAttrMult
		property string segment: "AnalyzeAttrMult"

		Common.PlanMultiple { suffix: analyzeAttrMult.segment }
		Common.Distribution { suffix: analyzeAttrMult.segment }

		Group
		{
			CheckBox { name: "assessPlan" + analyzeAttrMult.segment; label: qsTr("Assess Plan"); id: assessMult }
			Common.RiskPoints
			{
				suffix: analyzeAttrMult.segment
				enabled: assessMult.checked
			}
		}

		Common.ProbDefect { suffix: analyzeAttrMult.segment }
		Common.OutputOptions { suffix: analyzeAttrMult.segment }		
	}
}
