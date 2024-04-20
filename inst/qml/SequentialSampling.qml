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
	Common.ProbDefect { suffix: sequential.segment }
	// Common.Distribution {}
	// Todo: Ensure that the show plans option shows up before the other output options in the group.
	Common.OutputOptions
	{
		suffix: sequential.segment
		CheckBox { name: "showPlans" + sequential.segment; label: qsTr("Sequential Plans") }
	}
}